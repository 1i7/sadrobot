
function serverHttpRequest(url, params, onSuccess) {
    var req = new XMLHttpRequest();
    req.onreadystatechange = function() {
        if (req.readyState === 4) { // only if req is "loaded"
            if (req.status === 200) { // only if "OK"
                onSuccess(req.responseText);
            } else {
                // error
            }
        }
    };
    // can't use GET method here as it would quickly 
    // exceede max length limitation
    req.open("POST", url, true);

    //Send the proper header information along with the request
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
//    req.setRequestHeader("Content-length", params.length);
//    req.setRequestHeader("Connection", "close");
    req.send(params);
}
