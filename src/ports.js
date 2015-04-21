var app = Elm.fullscreen(Elm.Meetup, { now : new Date(Date.now()).toString() });

app.ports.showError.subscribe(function(msg) {
    alert(msg);
});

app.ports.getNow.subscribe(function() {
    app.ports.now.send(new Date(Date.now()).toString());
});
