/* JavaScript for the Whiteboard application */
var whiteboard = {
	pengine: undefined,
	
	startEngine: function() {
		this.pengine = new Pengine({
			application: 'whiteboard',
			ask: "new_whiteboard(12342349)",   // do you need the period?
			onsuccess: function() {
				whiteboard.writeln("success " + JSON.stringify(this.data));
			},
			onfailure: function() {
				whiteboard.writeln("failure");
			},
			onstop: function() {
				whiteboard.writeln("stopped");
			},
			onabort: function() {
				whiteboard.writeln("abort");
			},
			onerror: function() {
				whiteboard.writeln("error");
			}
		});
	},

	writeln: function(string) {
		$('#output').append(string + "<br />")
	},

	unchoose_tools: function() {
		$("#whiteboard .componentbar IMG").removeClass("selected");
	},

	newElement: function(e) {
			$("#msg").text("down " + e.clientX + " " + e.clientY);
	},

	newElementMoveOrDrag: function(e) {
		if (mouseDownCount === 0)
			return;
			
		var x = e.offsetX; 
		var y = e.offsetY; 
		
		$("#msg").text("drag " + x + " " + y + " " + e.button);
	},

	newElementCommit: function(e) {
	$("#msg").text("commit " + e.clientX + " " + e.clientY);
	}
}

$(document).ready(function() {
	whiteboard.startEngine();
	
	$("#rect_tool").on("click", function() {
		whiteboard.unchoose_tools();
		$("#rect_tool").addClass("selected");
	});
	$("#oval_tool").on("click", function() {
		whiteboard.unchoose_tools();
		$("#oval_tool").addClass("selected");
	});
	$("#diamond_tool").on("click", function() {
		whiteboard.unchoose_tools();
		$("#diamond_tool").addClass("selected");
	});

	$("#whiteboard .drawarea").on(
					{	"mousedown": whiteboard.newElement,
						"mousemove": whiteboard.newElementMoveOrDrag,
						"mouseup": whiteboard.newElementCommit});
});

var mouseDown = [0, 0, 0, 0, 0, 0, 0, 0, 0],
    mouseDownCount = 0;
document.body.onmousedown = function(evt) { 
  ++mouseDown[evt.button];
  ++mouseDownCount;
}
document.body.onmouseup = function(evt) {
  --mouseDown[evt.button];
  --mouseDownCount;
}
