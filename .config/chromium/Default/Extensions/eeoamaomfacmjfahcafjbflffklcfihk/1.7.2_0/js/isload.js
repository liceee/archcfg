var isLoad; 
if(typeof isLoad == 'undefined') {
	chrome.extension.sendRequest({name:'insert_script'});
} else {
	chrome.extension.sendRequest({name:'script_running'});
}