/*
	app.html, note.html
*/

var toBase64Table = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var base64Pad = '=';

function toBase64(data) {
  var result = '';
  var length = data.length;
  var i;
  // Convert every three bytes to 4 ascii characters.
  for (i = 0; i < (length - 2); i += 3) {
    result += toBase64Table[(data.charCodeAt(i) & 0xff) >> 2];
    result += toBase64Table[((data.charCodeAt(i) & 0x03) << 4) + ((data.charCodeAt(i+1) & 0xff) >> 4)];
    result += toBase64Table[((data.charCodeAt(i+1) & 0x0f) << 2) + ((data.charCodeAt(i+2) & 0xff) >> 6)];
    result += toBase64Table[data.charCodeAt(i+2) & 0x3f];
  }

  // Convert the remaining 1 or 2 bytes, pad out to 4 characters.
  if (length%3) {
    i = length - (length%3);
    result += toBase64Table[(data.charCodeAt(i) & 0xff) >> 2];
    if ((length%3) == 2) {
      result += toBase64Table[((data.charCodeAt(i) & 0x03) << 4) + ((data.charCodeAt(i+1) & 0xff) >> 4)];
      result += toBase64Table[(data.charCodeAt(i+1) & 0x0f) << 2];
      result += base64Pad;
    } else {
      result += toBase64Table[(data.charCodeAt(i) & 0x03) << 4];
      result += base64Pad + base64Pad;
   }
 }
 return result;
}

/*
function processImage(img) {
	var req;
	try {
		req = new XMLHttpRequest();
		req.open('GET', img.src, false);
		req.overrideMimeType('text/plain; charset=x-user-defined');
		req.send();
	}
	catch (e) {
		console.error('Image process error: ' + e.message);
	}
	
	if (req.readyState == 4 && req.status == 200) {
		img.src = 'data:' + req.getResponseHeader('Content-Type') 
			+ ';base64,' + toBase64(req.responseText);
	}
} */