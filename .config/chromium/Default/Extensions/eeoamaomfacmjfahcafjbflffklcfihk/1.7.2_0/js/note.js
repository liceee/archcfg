var noteId = 0, timerId = null;


var util = {
    isLink: function(link) {
        return (/http:|https:|ftp:/.test(link.split('/')[0])) ? true : false;
    },
    sendRequest: function(req) {
        chrome.extension.sendRequest(req);
    },
    updateTitle: function(t) {
        $('#title input').val(t);
    },
    updateSaveBtn: function(t) {
        $('#save').val(t);
    },
    getNoteDoc: function() {
        var doc = $('#editor iframe')[0].contentDocument;
        util.getNoteDoc = function() {
            return doc;
        };
        return doc;
    },
    getTopDoc: function() {
        var doc = top.document;
        util.getTopDoc = function() {
            return doc;
        };
        return doc;
    },
	cleanStyles: function(e) { //Function takes from readability
		e = e || document;
		var cur = e.firstChild;

		// If we had a bad node, there's not much we can do.
		if(!e)
			return;

		// Remove any root styles, if we're able.
		if(typeof e.removeAttribute == 'function')
			e.removeAttribute('style');

		// Go until there are no more child nodes
		while ( cur != null ) {
			if ( cur.nodeType == 1 ) {
				// Remove style attribute(s) :
				cur.removeAttribute("style");
				util.cleanStyles( cur );
			}
			cur = cur.nextSibling;
		}
	},
    addSelection: function(s) {
        top.postMessage('maximize', '*');      
        if(!s) return;
				$(util.getNoteDoc().body).append(s);
				//util.cleanStyles(null);
				note.save({data:'desc', selection:s});
    },
    disableAutoType: function() {
        $('#title>input').addClass('typed');
        util.disableAutoType = function(){};
    }
};	

var note = {
	init: function(request) {
		var item = request.item;
		$('#editor>textarea').rte({
			css: ['css/text.css', 'css/notearea.css'],
			width: 260
		}); 
		
		var noteFrame = $('#editor>.rte-zone>iframe')[0],
			noteDoc = noteFrame.contentDocument,
			noteBody = noteDoc.body;
		$('#title>input').bind('textchange', 'title', function(e) {
			util.disableAutoType();
			note.save(e);
		});
		$(noteBody).bind('textchange', 'desc', function(e) {
			util.cleanStyles(this); //this->noteBody
			note.save(e);
		});
		$(noteDoc).keyup(function(e) {
			if(e.keyCode==13) util.disableAutoType();
		});
		
		$('#action').click(function(e) {
			if (e.target.tagName!='LI') return;
			
			var action = e.target.id;
			if (action == 'app') 
				return util.sendRequest({name:'open_app'});
			setTimeout(function() {$('body')[0].className = action}, action=='maximize'?100:0); //avoid close button appears before notepad streching fully
			
			top.postMessage(action, '*');
		});
		
		
//		console.log('100',item);
		if (item) {
			$('#title input').val(item.title);
			noteBody.innerHTML = item.desc+request.selection;
			//$(noteBody).html(item.desc);
			noteId = parseInt(item.id);
			//util.sendRequest({name:'get_selection'});
			note.save({data:'desc'});
		} 
		else 
			note.save({data:'add'});
	},
    processImages: function(callback) {
        var doc = util.getNoteDoc();
        var i = 0,
        imgs = doc.getElementsByTagName('img');
        len = imgs.length;
        if (!len) {
            callback();
            return;
        }
        var img = imgs[i];
		
        function processImage() {
            if (!util.isLink(img.src)) {
                processNext();
                return;
            }
            chrome.extension.sendRequest({
                name:'process_image',
                href:img.src,
                binary:true
            },
            function(res) {
								if (util.isLink(img.src)) img.dataset['src'] = img.src;
                img.src = 'data:' + res.contentType+ ';base64,' + toBase64(res.data);
                processNext();
            }
            );
        }
        function processNext() {
            if (img=imgs[++i]) processImage();
            else callback();
        }
		
        processImage();
    },
    save: function(e) {
        var reqName = e.data;//, data = '', delay = 0;
		
        function saveDesc() {
            timerId = setTimeout(
                function(req) {
                    util.sendRequest(req);
                    util.updateSaveBtn('Saving...');
                },
                delay,
                {
                    name:reqName,
                    id:noteId,
                    data: $($('#editor>.rte-zone>iframe')[0].contentDocument.body).html()
                }
                );
        }
        switch(reqName) {
            case 'add':
            //case 'delete':
                util.sendRequest({
                    name:reqName,
                    id:noteId
                });
                break;
            case 'title':
            case 'url':
                util.sendRequest({
                    name:reqName,
                    id:noteId,
                    data:e.text || (e.target&&e.target.value) || ''
                    });
                break;
            case 'desc':
                note.updateTitle();
                clearTimeout(timerId);
                timerId = null;
                delay = 1000;
                note.processImages(saveDesc);
                break;
        }
    },
    updateTitle: function() {
        var $t = $('#title input');
        if ($t.hasClass('typed')) return;
		
        var doc = util.getNoteDoc();
        var text = doc.body.textContent;
        if (text.length>30) text = text.substr(0, 30);
        $t.val(text);
        note.save({
            data:'title',
            id:noteId,
            text:text
        });
    },
    updatedContent:function(request){
        var item = request.item,
            noteFrame = $('#editor>.rte-zone>iframe')[0],
            noteDoc = noteFrame.contentDocument,
            noteBody = noteDoc.body;
        if(item){
            $('#title input').val(item.title);
            noteBody.innerHTML = item.desc;
        }
    }
}

chrome.extension.onRequest.addListener(function(request, sender, sendResponse) {
    switch(request.name) {
		case 'check_url':
			note.init(request);
			break;
        case 'raloadNote':
            note.updatedContent(request);
            break;
		case 'add':
			noteId = parseInt(request.id);
			if (request.selection) {
				$($('#editor>.rte-zone>iframe')[0].contentDocument.body)
					.html(request.selection.replace(/\<div\>\<br \/\>\<\/div>/, ''))
					.trigger('textchange');
			}
			// 
			/* $('#title input').val(item.title);
			noteBody.innerHTML = item.desc+item.selection;
			noteId = parseInt(item.id);
			note.save({name:'desc', target:noteBody}); */
			break;
		case 'save_note':
				break;
		case 'delete':
				note.save({data:'add'});
				break;
		case 'get_selection':
				util.addSelection(request.selection);
				break;
		case 'action':
			document.body.className = request.action.replace('diigo_note_app_', '');
			break;
    }
    util.updateSaveBtn('Saved');
});

$(document).ready(function() {
	top.postMessage('check_url', '*');		// send msg to content.js
});