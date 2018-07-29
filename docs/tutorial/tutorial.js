// from w3schools, somewhere

function idElement(elem) {
    if(typeof elem === "string") {
        return document.getElementById(elem);
    }
    else {
        return elem;
    }
}

function openNav(name, toClose) {
    idElement(name).style.width = "250px";
    idElement(toClose).style.marginLeft = "250px";
}

function closeNav(name, toClose) {
    idElement(name).style.width = "0";
    idElement(toClose).style.marginLeft = "0";
}
