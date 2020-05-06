<?php
?>

<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Cypria Querying Language</title>
  <!-- Favicon -->
  <link rel="shortcut icon" href="images/favicon.ico" type="image/x-icon">
  <link rel="icon" href="images/favicon.ico" type="image/x-icon">
  <!-- CSS -->
  <link rel="stylesheet" type="text/css" href="css/main.css">
  <link rel="stylesheet" type="text/css" href="css/nav.css">
  <link rel="stylesheet" type="text/css" href="css/index.css">
  <!-- Scripts -->
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"></script>
  <script src="https://unpkg.com/imagesloaded@4/imagesloaded.pkgd.min.js"></script>
  <script type="text/javascript" src="scripts/main.js"></script>
  <!-- Fonts -->
  <link rel="stylesheet" type="text/css" href="fonts/roboto-normal.css">
  <link rel="stylesheet" type="text/css" href="fonts/roboto-light.css">
  <link rel="stylesheet" type="text/css" href="fonts/roboto-bold.css">
  <!-- Keywords, names, properties go here later -->
</head>

<body>
  <!-- If browser has JS disabled -->
  <?php include("includes/noscript.php"); ?>

  <!-- Navigation bar -->
  <nav>
    <?php include("includes/nav.php"); ?>
  </nav>

  <!-- Content -->
  <div class="main-container">
    <div></div>
    <div class="main">
      <div class="intro section">
        <div id="intro-description">
          Cypria is an open-source query language that interprets to SQL,
          making it easy to build complicated queries with
          <span>functional</span> constructs.
        </div>
        <div id="intro-image"></div>
        <div id="intro-download">Download Cypria (coming soon)</div>
      </div>
      <div class="playground section">
        <h1>Try Cypria</h1>
        <div id="code-box">
          <textarea>
(* Try writing some code here! *)
let tbl = "products" in
tbl |> filter_max ["name";"price"] "price"&#13;&#10;</textarea>
        </div>
        <div id="code-output">
          <pre><span class="output">SELECT name, MAX(price) as price FROM products</span></pre>
        </div>
        <button id="run-button">Run code</button>
      </div>
      <div class="links section">
        <h1>Documentation & links</h1>
        <ul>
          <li>
            <a href="https://github.com/CornellDataScience/cypria-query-language/blob/master/cypria_planning/language_spec.md">
              Cypria documentation
            </a>
          </li>
          <li>
            <a href="https://github.com/CornellDataScience/cypria-query-language">
              GitHub repository
            </a>
          </li>
          <li>
            <a href="https://github.com/CornellDataScience/cypria-query-language">
              Foo
            </a>
          </li>
          <li>
            <a href="https://github.com/CornellDataScience/cypria-query-language">
              Bar
            </a>
          </li>
          <li>
            <a href="https://github.com/CornellDataScience/cypria-query-language">
              Baz
            </a>
          </li>
        </ul>
      </div>
      <div class="other section">
        <h1>Reserved space</h1>
      </div>
    </div>

    <!-- Footer -->
    <footer>
      <?php include("includes/footer.php"); ?>
    </footer>
  </div>
</body>

</html>