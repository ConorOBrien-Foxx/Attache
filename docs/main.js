// helper js functions for documentation page

function normalizeGenre(genre) {
    return genre
        .replace(/[()]/g, "")
        .replace(/ /g, "_")
        .replace(/\//g, "-");
}

var functions, rows, cache = [], included = new Set();
function removeRow(index) {
    cache[index] = rows[index];
    rows[index].remove();
}
function restoreRow(i) {
    if(!cache[i]) {
        return;
    }
    var addto = i + 1;
    if (i === 0) {
        functions.insertBefore(cache[i], functions.firstChild);
    }
    if (i === rows.length) {
        functions.appendChild(cache[i]);
    }
    // find index of element not in cache
    while (cache[addto]) {
        addto++;
    }
    functions.insertBefore(cache[i], rows[addto]);
    delete cache[i];
}

function updateRows() {
    rows.forEach(function(el, i) {
        var sourceEl = el.querySelector(".genre");
        var genreEl = normalizeGenre(sourceEl.textContent);
        
        if(included.size === 0) {
            restoreRow(i);
            el.classList.remove("focused");
        }
        else if(included.has(genreEl)) {
            el.classList.add("focused");
            restoreRow(i)
        }
        else {
            el.classList.remove("focused");
            removeRow(i);
        }
    });
}

window.addEventListener("load", function() {
    document.querySelectorAll(".source-button").forEach(function(button) {
        var sourceId = button.id.replace("-button", "-source");
        var source = document.getElementById(sourceId);
        var hidden = true;
        button.addEventListener("click", function() {
            if(hidden) {
                source.style.height = source.scrollHeight + "px";
            }
            else {
                source.style.height = "0px";
            }
            
            hidden = !hidden;
        });
    });
    
    functions = document.getElementById("functions");
    // var rows = functions.children;
    rows = document.querySelectorAll(".function");
    var sources = document.querySelectorAll(".genre-source");
    sources.forEach(function(source) {
        var genre = normalizeGenre(source.textContent);
        
        // is this genre focused?
        var focused = false;
        
        source.addEventListener("click", function() {
            console.log("Clicked on " + genre);
            focused = !focused;
            if(focused) {
                included.add(genre);
            }
            else {
                included.delete(genre);
            }
            lastFocus = genre;
            
            source.style.backgroundColor = focused ? "#C6EAEA" : "";
            
            updateRows();
        });
    });
});