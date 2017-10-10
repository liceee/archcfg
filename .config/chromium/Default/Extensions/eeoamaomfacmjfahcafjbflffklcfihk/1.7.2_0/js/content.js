var isLoad = true;

function hasClass(ele,cls) {
	return ele.className.match(new RegExp('(\\s|^)'+cls+'(\\s|$)'));
}
function addClass(ele,cls) {
	if (!this.hasClass(ele,cls)) ele.className += " "+cls;
}
function removeClass(ele,cls) {
	if (hasClass(ele,cls)) {
		var reg = new RegExp('(\\s|^)'+cls+'(\\s|$)');
		ele.className=ele.className.replace(reg,' ');
	}
}

function getSidebar() {
	var s = document.getElementById('note_diigo');
	getSidebar = function() { return s; };
	return s;
}

function handleKeydown(e) {
	var t = e.target,
		n = t.tagName;
	if (e.keyCode==84 && n!='INPUT' && n!='TEXTAREA' && !t.isContentEditable) {
		init();
	}
}

function init(request) {
	//document.body.innerHTML += '<div id="note_diigo" class="diigo_note_app_maximize"><iframe src="'+chrome.extension.getURL('note.html')+'" frameborder="0" id="note_wrap_diigo"></iframe></div>';
	var newiframe = document.createElement('iframe');
	newiframe.src=chrome.extension.getURL('note.html');
	newiframe.setAttribute('frameborder','0');
	newiframe.id = 'note_wrap_diigo';
	var newdiv = document.createElement('div');
	newdiv.id="note_diigo";
	newdiv.className="diigo_note_app_maximize";
	newdiv.appendChild(newiframe);
	document.body.appendChild(newdiv);
	
	
	window.addEventListener('keydown', handleKeydown, false);
	
	init = function(request) {
		var s = getSidebar(),
			c = s.className;
		
		if (getSelectionHTML()) { // from menu.js - check if has selection
			chrome.extension.sendRequest({name:'get_selection'});
		}
		else {
			switch(c) {
			case 'diigo_note_app_maximize':
				c = 'diigo_note_app_minimize';
				break;
			case 'diigo_note_app_minimize':
				c = 'diigo_note_app_maximize';
				break;
			case 'diigo_note_app_close':
				c = 'diigo_note_app_maximize';
				break;
			}
			s.className = c;
			chrome.extension.sendRequest({name:'action', action:c});
		}
		//hasClass(s, 'hide')||!getSelection().isCollapsed ? removeClass(s, 'hide') : addClass(s, 'hide');
	};
	//document.getElementById('note_handler_diigo').addEventListener('click', init, false);
}

chrome.extension.onRequest.addListener(function(request, sender, sendResponse) {
	switch(request.name) {
	case 'init':
		init(request);
		break;
	/* case 'action':
		getSidebar().className = 'diigo_note_app_'+request.action;
		break; */
	}
});

window.addEventListener("message", function(e){ 		// listen msg from note.js
	if (e.data == 'check_url')
		chrome.extension.sendRequest({name: 'check_url', url: location.href.replace(location.hash, '')});
	else if (e.data == 'maximize') {
		var s = getSidebar();
		s.className = "diigo_note_app_maximize";
		chrome.extension.sendRequest({name:'action', action:'diigo_note_app_maximize'});
	}else
    {
        if(e.data!="maximize" && e.data!="minimize" && e.data!="close") return;
		getSidebar().className = 'diigo_note_app_'+e.data;
    }
}, false);