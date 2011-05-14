
var socket = null;

function onOpen() {
    socket.send({
        "type": "join",
        "nick": $("#nick").val()
    });
}

function onClose() {
}

function onData(data) {
    data = eval("(" + data + ")");
    console.log(data);

    switch (data.type) {
    case "message":
        message = "<strong>" + data.user + "</strong>" + " " + data.message;
        $("#view").append(message + "<br />");
        break;
    case "nickChange":
        message = data.oldNick + " changed its nickname to " + data.newNick;
        $("#view").append("<em>" + message + "</em>" + "<br />");
        break;
    case "userList":
        $("#userList").empty();
        for (var i = 0; i < data.users.length; i++) {
            $("#userList").append("<li>" + data.users[i] + "</li>");
        }
        break;
    case "userJoined":
        message = data.user + " joined this chan";
        $("#view").append("<em>" + message + "</em>" + "<br />");
        break;
    }
}

function setNick(nick) {
    socket.send({
        "type": "setNick",
        "nick": nick
    });
}

function sendMessage(user, message) {
    socket.send({
        "type": "message",
        "message": message,
        "user": user
    });
}

$(document).ready(function() {
    jsSocket.swf = "jsSocket.swf";

    socket = new jsSocket({
        hostname: "127.0.0.1",
        debug: true,
        port: 8080,
        path: "@chat",
        onOpen: onOpen,
        onData: onData,
        onClose: onClose
    });

    socket.logger = Function.prototype.bind.call(console.log, console);

    $("#setNick").click(function() {
        setNick($("#nick").val());
    });

    $("#send").click(function() {
        sendMessage($("#nick").val(), $("#input").val());
        $("#input").val("");
    });
});
