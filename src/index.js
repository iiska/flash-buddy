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
    if (cached) {
      app.ports.restoredStateCache.send(JSON.parse(cached));
    }
  } catch (e) {
    console.warn(`Encountered: ${e}, clearing local cache`);
    localStorage.removeItem(stateCacheKey);
  }
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
