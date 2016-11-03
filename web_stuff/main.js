
var chooser = webapp.WebApp();

$.get("../data/data_structures.txt", function (dataStructuresText) {
  $("#structures").val(dataStructuresText);
  $.get("../data/implementations.txt", function (implsText) {
    $("#impls").val(implsText);
    $("#analyse-button").on("click", function(e) {
      console.log("calculating");
      $("#result").html("<p>calculating! (page will be non-responsive)</p>");
      setTimeout(function () {
        var adtString = $("#adt").val().trim();

        var startTime = Date.now();

        try {
          var choices = chooser.makeChoices($("#impls").val(), $("#structures").val(), adtString);
        } catch (e) {
          $("#result").html("<p>Error: " + e.toString() + "</p><p>" + e.stack + "</p>");
          debugger;
          return;
        }

        console.log((Date.now() - startTime) / 1000);

        var resultString = choices.map((choice) => {
          return (
            "<p>Here are your optimal composite data structures!</p>" +
            "<h4>"+choice.choices.join(", ") + "</h4>\n" +
            "<p><ul>" + Object.keys(choice.times).map((methodName) =>
              "<li>" + methodName + " <- " + choice.times[methodName] + "</li>").join("\n") +
            "</ul></p>")
        }).join("\n");

        $("#result").html(resultString);
      }, 1);
    })
  })
});
