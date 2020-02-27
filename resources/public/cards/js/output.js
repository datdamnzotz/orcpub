function receiveMessage(event) {
    var html = event.data;
    insertCards(html);
}

function insertCards(html) {
    // Remove all previous content
    while (document.body.hasChildNodes()) {
        document.body.removeChild(document.body.lastChild);
    }

    // Create a div that holds all the received HTML
    var div = document.createElement("div");
    div.setAttribute("class", "output-container");
    div.id = "output-container";
    div.innerHTML = html;

    // Add the new div to the document
    document.body.appendChild(div);
}

window.addEventListener("message", receiveMessage, false);
