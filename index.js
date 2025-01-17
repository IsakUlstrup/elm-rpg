import { Elm } from "./src/Main.elm";
Elm.Main.init({ node: document.getElementById("app") });


const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
const map = urlParams.get('map')


const response = await fetch(`public/${map}.json`);
const file = await response.json();
console.log("map file", file);
