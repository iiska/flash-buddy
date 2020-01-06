import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

function storageAvailable(type) {
  var storage;
  try {
    storage = window[type];
    var x = "__storage_test__";
    storage.setItem(x, x);
    storage.removeItem(x);
    return true;
  } catch (e) {
    return (
      e instanceof DOMException &&
      // everything except Firefox
      (e.code === 22 ||
        // Firefox
        e.code === 1014 ||
        // test name field too, because code might not be present
        // everything except Firefox
        e.name === "QuotaExceededError" ||
        // Firefox
        e.name === "NS_ERROR_DOM_QUOTA_REACHED") &&
      // acknowledge QuotaExceededError only if there's something already stored
      storage &&
      storage.length !== 0
    );
  }
}

const stateCacheKey = "flashBuddyState";
Object.freeze(stateCacheKey);

const app = Elm.Main.init({
  node: document.getElementById("root")
});

if (storageAvailable("localStorage")) {
  app.ports.storeStateCache.subscribe(data => {
    localStorage.setItem(stateCacheKey, JSON.stringify(data));
  });

  try {
    const cached = localStorage.getItem(stateCacheKey);
    const data = cached ? JSON.parse(cached) : { localStorage: "available" };
    console.log(data);
    app.ports.restoredStateCache.send(data);
  } catch (e) {
    console.warn(`Encountered: ${e}, clearing local cache`);
    localStorage.removeItem(stateCacheKey);
  }
} else {
  app.ports.restoredStateCache.send({ localStorage: "not_available" });
}

serviceWorker.register({
  onUpdate: () => {
    app.ports.restoredStateCache.send({ serviceWorker: "update_available" });
  }
});
