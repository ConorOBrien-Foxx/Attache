// from w3schools, somewhere

function idElement(elem) {
    if(typeof elem === "string") {
        return document.getElementById(elem);
    }
    else {
        return elem;
    }
}

function openNav(name) {
    idElement(name).style.width = "250px";
}

function closeNav(name) {
    idElement(name).style.width = "0";
}
