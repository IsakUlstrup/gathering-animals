import { Elm } from '../src/Main.elm';

const storedState = localStorage.getItem('inventory');

const config = {
  saveData: storedState ? JSON.parse(storedState) : null,
  time: Date.now()
};


const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: config
});

app.ports.storeInventory.subscribe(function (items) {
  localStorage.setItem('inventory', items);
});
