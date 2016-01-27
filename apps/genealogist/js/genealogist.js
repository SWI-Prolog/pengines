/* JavaScript for the Genealogist application */

var pengine;

function ask() {
    var query = $("#query").val();
    if (query) {
        pengine = new Pengine({
            application: 'genealogist',
            ask: query,
            onsuccess: function() {
                writeln(JSON.stringify(this.data));
                if (this.more) {
                    disableButtons(true, false, false, false);
                } else {
                    writeln("No more solutions");
                    disableButtons(false, true, true, true);                        
                }
            },
            onfailure: function() {
                writeln("Failure");
                disableButtons(false, true, true, true);
            },
            onstop: function() {
                writeln("Stopped");
                disableButtons(false, true, true, true);
            },
            onabort: function() {
                writeln("Aborted");
                disableButtons(false, true, true, true);
            },
            onerror: function() {
                writeln("Error: " + this.data);
                disableButtons(false, true, true, true);
            }
        });
    }
}

function next() {
    pengine.next();
}

function stop() {
    pengine.stop();
}

function abort() {
    pengine.abort();
}


function update(op) {
    var pred = op + $("input[name=sex]:checked").val(),
        X = $("#X").val().toLowerCase() || '_',
        Y = $("#Y").val().toLowerCase() || '_',
        command = pred + '(' + X + ',' + Y + ')';
    pengine = new Pengine({
        application: 'genealogist',
        ask: command,
        onsuccess: function() {
            writeln(command);
            $("#X,#Y").val("");
        },
        onerror: function() {
            writeln("Error: " + this.data);
        }
    });
}


function writeln(string) {
    $('#output').append(string + "<br />");
}

function disableButtons(ask, next, stop, abort) {
    $("#ask-btn").prop("disabled", ask);
    $("#next-btn").prop("disabled", next);
    $("#stop-btn").prop("disabled", stop);
    $("#abort-btn").prop("disabled", abort);
}


$(document).ready(function() {
    $("#sample-queries").on("change", function() {
        $("#query").val($("#sample-queries option:selected").text());
    });
    $("#ask-btn").on("click", ask);
    $("#next-btn").on("click", next);
    $("#stop-btn").on("click", stop);
    $("#abort-btn").on("click", abort);
    $("#assert-btn").on("click", function() {
        update('assert_');
    });
    $("#retract-btn").on("click", function() {
        update('retract_');
    });
    $("#clear-btn").on("click", function() {
        $('#output').html('');
    });
});

