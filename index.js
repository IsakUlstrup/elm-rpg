import { Elm } from "./src/Main.elm";

const app = Elm.Main.init({ node: document.getElementById("app") });

app.ports.requestChunk.subscribe(function (chunk) {
    // console.log("request chunk:", chunk);

    fetch(`map/${chunk}.json`)
        .then((response) => {
            if (response.ok) {
                return response.json();
            }
            throw new Error('Something went wrong');
        })
        .then((json) => {
            // console.log("sending chunk", chunk, json);
            app.ports.gotChunk.send(json);
        })
        .catch((error) => {
            console.log("Chunk not found:", chunk)
        });
});
