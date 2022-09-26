import { Elm } from '../src/Main.elm';

const storedState = localStorage.getItem('inventory');
// console.log("JS: Retrieved state: ", storedState);
const startingState = storedState ? JSON.parse(storedState) : null;

const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: startingState
});

app.ports.storeInventory.subscribe(function (items) {
  if (items.length > 0) {
    const itemsJson = JSON.stringify(items);
    localStorage.setItem('inventory', itemsJson);
    // console.log("JS: Saved state: ", itemsJson);
  }
});
