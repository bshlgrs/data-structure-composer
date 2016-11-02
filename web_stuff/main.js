console.log("hey");

var chooser = webapp.WebApp();

$.get("/data/data_structures.txt", function (dataStructuresText) {
  $("#structures").val(dataStructuresText);
  $.get("/data/implementations.txt", function (implsText) {
    $("#impls").val(implsText);
    $("#analyse-button").on("click", function(e) {
      var adtString = $("#adt").val().trim();
      var choices = chooser.makeChoices($("#impls").val(), $("#structures").val(), adtString);

      var resultString = choices.map((choice) => {
        return (
          "<h4>"+choice.choices.join(", ") + "</h4>\n" +
          "<p><ul>" + Object.keys(choice.times).map((methodName) =>
            "<li>" + methodName + " <- " + choice.times[methodName] + "</li>").join("\n") +
          "</ul></p>")
      }).join("\n");

      $("#result").html(resultString);
    })
  })
});
