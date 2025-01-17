import { Elm } from "./src/Main.elm";
Elm.Main.init({ node: document.getElementById("app") });


const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
const map = urlParams.get('map')



fetch(`public/${map}.json`)
    .then((response) => response.json())
    .then((json) => console.log(json));