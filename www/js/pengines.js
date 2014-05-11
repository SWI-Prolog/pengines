/*  Copyright (c) 2014, Torbjörn Lager
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

function Pengine(callbacks) {
    var goal = callbacks.goal;
    var src = callbacks.src ? callbacks.src : "";
    var format = callbacks.format ? callbacks.format : "json";
    var server = callbacks.server !== undefined ? callbacks.server : "/";
    this.id = null;
    var that = this;
    // Private functions
    function source() {
        var scripts = document.getElementsByTagName('script');
        var src = "";
        for (var i = 0; i < scripts.length; i++) {
            if (scripts[i].getAttribute('type') == 'text/x-prolog') {
                src += '\n' + scripts[i].textContent;
            }
        }
        return src;
    }
    function options_to_list(options) {
        var opts = "[";
        for (var i in options) {
            opts += i + "(" + options[i] + "),";
        }
        if (opts.length > 1) {
            opts = opts.slice(0, -1);
        }
        return opts + "]";
    }
    function process_response(obj) {
        if (obj.event === 'create') {
            that.id = encodeURIComponent(obj.id);
            if (callbacks.oncreate) callbacks.oncreate.call(obj);
        } else if (obj.event === 'stop') {
            if (callbacks.onstop) callbacks.onstop.call(obj);
        } else if (obj.event === 'success') {
            if (callbacks.onsuccess) callbacks.onsuccess.call(obj);
        } else if (obj.event === 'failure') {
            if (callbacks.onfailure) callbacks.onfailure.call(obj);
        } else if (obj.event === 'error') {
            if (callbacks.onerror)
	        callbacks.onerror.call(obj);
	    else if (typeof(console) !== 'undefined')
	        console.error(obj.data);
        } else if (obj.event === 'output') {
            if (callbacks.onoutput) callbacks.onoutput.call(obj);
            that.pull_response();
        } else if (obj.event === 'debug') {
            if (callbacks.ondebug)
	        callbacks.ondebug.call(obj);
	    else if (typeof(console) !== 'undefined')
		console.log(obj.data);
            that.pull_response();
        } else if (obj.event === 'prompt') {
            if (callbacks.onprompt) callbacks.onprompt.call(obj);
        } else if (obj.event === 'abort') {
            if (callbacks.onabort) callbacks.onabort.call(obj);
        } else if (obj.event === 'destroy') {
            if (callbacks.ondestroy) callbacks.ondestroy.call(obj);
        }
    };
    // Public functions
    this.send = function(event) {
        var event = encodeURIComponent(event);
        $.get(server + 'pengine/send?id=' + that.id +
	      '&event=' + event + '&format=' + format, process_response);
    }
    this.ask = function(query, options) {
        that.send('request(ask(' + query + ', ' + options_to_list(options) + '))');
    }
    this.input = function(event) {
        that.send('input(' + event + ')');
    }
    this.next = function() {
        that.send('request(next)');
    }
    this.stop = function() {
        that.send('request(stop)');
    }
    this.destroy = function() {
        that.send('request(destroy)');
    }
    this.pull_response = function() {
        $.get(server + 'pengine/pull_response?id=' + that.id +
	      '&format=' + format, process_response);
    }
    this.abort = function() {
        $.get(server + 'pengine/abort?id=' + that.id +
	      '&format=' + format, process_response);
    }

    $.ajax(server + 'pengine/create',
	   { "contentType": "application/json; charset=utf-8",
	     "dataType": "json",
	     "data": JSON.stringify({ src_text: source(),
				      format: format
				    }),
	     "success": process_response,
	     "type": "POST"
	   });
}
