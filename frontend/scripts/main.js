$(document).ready(function () {
  /* Sends HTTP request to language server for code interpretation */
  $("#run-button").click(function () {
    let $code = $.trim($("#code").val());
    $code = $code.replace(/\r?\n|\r/g, " ");

    $.ajax({
      url: "http://lang.cypriaql.com/cypria",
      type: "POST",
      data: {
        cypria_raw: $code
      }
    }).done(function (res) {
      $("pre").html(`<code>${res}</code>`);
    }).fail(function (jqXHR, err) {
      $("pre").text(`Request failed with:\n${jqXHR.status} ${jqXHR.statusText}`);
    });
  });

  /* Toggles navigation bar in smaller screens */
  $(".nav-toggle").click(() => {
    $(".item").toggleClass("shown");
  });
});