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

function Pengine(options) {
    if ( typeof Pengine.ids == 'undefined' ) {
        Pengine.ids = [];
    }
    var src = options.src ? options.src : "";
    var format = options.format ? options.format : "json";
    var server = options.server !== undefined ? options.server : "/";
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
            Pengine.ids.push(obj.id);
            if (Pengine.ids.length > obj.data.slave_limit) {
                alert("Attempt to use too many slave pengines. The limit is :" + obj.data.slave_limit);
                Pengine.destroy_all();
                Pengine.ids = [];
            } else {
                that.id = encodeURIComponent(obj.id);
                if (options.oncreate && that.id != "null") options.oncreate.call(obj.data.answer);
                if (obj.data.answer) process_response(obj.data.answer);
            }
        } else if (obj.event === 'stop') {
            if (options.onstop) options.onstop.call(obj);
        } else if (obj.event === 'success') {
            if (options.onsuccess) options.onsuccess.call(obj);
        } else if (obj.event === 'failure') {
            if (options.onfailure) options.onfailure.call(obj);
        } else if (obj.event === 'error') {
            if (obj.data === "too_many_pengines") alert("too many pengines")
            if (options.onerror) options.onerror.call(obj);
	    else if (typeof(console) !== 'undefined')
	        console.error(obj.data);
        } else if (obj.event === 'output') {
            if (options.onoutput) options.onoutput.call(obj);
            that.pull_response();
        } else if (obj.event === 'debug') {
            if (options.ondebug) options.ondebug.call(obj);
	    else if (typeof(console) !== 'undefined')
		    console.log(obj.data);
            that.pull_response();
        } else if (obj.event === 'prompt') {
            if (options.onprompt) options.onprompt.call(obj);
        } else if (obj.event === 'abort') {
            if (options.onabort) options.onabort.call(obj);
        } else if (obj.event === 'destroy') {
            if (options.ondestroy) options.ondestroy.call(obj);
            if (obj.data) process_response(obj.data)
        }
    };
    function send(event) {
        var event = encodeURIComponent(event);
        $.get(server + 'pengine/send?id=' + that.id +
	      '&event=' + event + '&format=' + format, process_response);
    }
    // Public functions
    this.ask = function(query, options) {
        send('request(ask(' + query + ', ' + options_to_list(options) + '))');
    }
    this.next = function() {
        send('request(next)');
    }
    this.stop = function() {
        send('request(stop)');
    }
    this.respond = function(input) {
        send('input(' + input + ')');
    }
    this.pull_response = function() {
        $.get(server + 'pengine/pull_response?id=' + that.id +
	      '&format=' + format, process_response);
    }
    this.abort = function() {
        $.get(server + 'pengine/abort?id=' + that.id +
	      '&format=' + format, process_response);
    }
    this.destroy = function() {
        $.get(server + 'pengine/destroy?id=' + that.id +
	      '&format=' + format, process_response);
    }
    // Static functions
    Pengine.destroy_all = function() {
        $.ajax({url:server + 'pengine/destroy_all?ids=' + Pengine.ids, async:false})
    }; 
    // On creation
    var createOptions = {};
    createOptions["src_text"] = source() + "\n" + src;
    createOptions["format"] = format;
    if (options.application) createOptions["application"] = options.application;
    if (options.ask) createOptions["ask"] = options.ask;
    if (options.template) createOptions["template"] = options.template;
    if (options.chunk) createOptions["chunk"] = options.chunk;
    if (typeof options.destroy == "boolean" ) createOptions["destroy"] = options.destroy;
    $.ajax(server + 'pengine/create',
	   { "contentType": "application/json; charset=utf-8",
	     "dataType": "json",
	     "data": JSON.stringify(createOptions),
	     "success": process_response,
	     "type": "POST"
	   });
}

window.onunload = function() {
    try {
        Pengine.destroy_all();
    } catch(e) {}
};


