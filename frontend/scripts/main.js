$(document).ready(function () {
  /* Sends HTTP request to language server for code interpretation */
  $("#run-button").click(function () {
    let $code = $.trim($("#code").val());
    $code = $code.replace(/\r?\n|\r/g, " ");

    $.ajax({
      url: "http://lang.cypriaql.com/cypria",
      // url: "https://reqres.in/api/users/2",
      type: "POST",
      data: {
        cypria_raw: $code
      },
      // dataType: "jsonp",
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*"
      }
    }).done(function (res) {
      $("pre").html(`<code>${JSON.stringify(res, undefined, 2)}</code>`);
    }).fail(function (jqXHR, err) {
      $("pre").text(`Request failed with:\n${jqXHR.status} ${jqXHR.statusText}`);
    });
  });

  /* Toggles navigation bar in smaller screens */
  $(".nav-toggle").click(() => {
    $(".item").toggleClass("shown");
  });
});