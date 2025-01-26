import { Elm } from "./src/Main.elm";

const app = Elm.Main.init({ node: document.getElementById("app") });

app.ports.requestChunk.subscribe(function (position) {
    // console.log("request chunk:", chunk);
    const q = position[0];
    const r = position[1]

    fetch(`map/(${q}, ${r}).json`)
        .then((response) => {
            if (response.ok) {
                return response.json();
            }
            return Promise.reject("chunk not found");
            // throw new Error('Something went wrong');
        })
        .then((json) => {
            // console.log("sending chunk", chunk, json);
            app.ports.gotChunk.send(
                {
                    "q": q,
                    "r": r,
                    "tiles": json
                }
            );
        })
        .catch((error) => {
            // console.log("Chunk not found:", chunk)
            app.ports.gotChunk.send(
                {
                    "q": q,
                    "r": r,
                    "tiles": null
                }
            );
        });
});
