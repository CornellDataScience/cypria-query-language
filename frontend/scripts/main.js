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

  /* Lined text area for playground - adapted from golang.org */
  $.fn.addRowNums = function () {
		/*
		 * Helper function to make sure the line numbers are always kept up to
		 * the current system
		 */
    var fillOutLines = function (rowNumbers, h, lineNo) {
      while (rowNumbers.height() < h) {
        rowNumbers.append("<div>" + lineNo + "</div>");
        lineNo++;
      }
      return lineNo;
    };

    return this.each(function () {
      var lineNo = 1;
      var textarea = $(this);

      /* Wrap the text area in the elements we need */
      // textarea.wrap("<div class='linedtextarea' style='height:100%; overflow:hidden'></div>");
      textarea.parent().prepend("<div id='row-numbers'></div>");
      var rowNumbers = textarea.parent().find("#row-numbers");

      var scroll = function (tn) {
        var domTextArea = $(this)[0];
        var scrollTop = domTextArea.scrollTop;
        var clientHeight = domTextArea.clientHeight;
        rowNumbers.css({
          'margin-top': (-scrollTop) + "px"
        });
        lineNo = fillOutLines(rowNumbers, scrollTop + clientHeight,
          lineNo);
      };
      /* React to the scroll event */
      textarea.scroll(scroll);
      $(window).resize(function () { textarea.scroll(); });
      /* We call scroll once to add the line numbers */
      textarea.scroll();
    });
  };
});

$(function () {
  $(".code-text").addRowNums();
});