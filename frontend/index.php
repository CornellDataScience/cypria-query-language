<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Cypria Querying Language</title>
  <!-- Favicon -->
  <?php include("includes/favicon.php") ?>
  <!-- CSS -->
  <link rel="stylesheet" type="text/css" href="css/main.css">
  <link rel="stylesheet" type="text/css" href="css/nav.css">
  <link rel="stylesheet" type="text/css" href="css/index.css">
  <!-- Scripts -->
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"></script>
  <script type="text/javascript" src="scripts/main.js"></script>
  <!-- Fonts -->
  <link rel="stylesheet" type="text/css" href="fonts/roboto.css">
  <link rel="stylesheet" type="text/css" href="fonts/montserrat.css">
  <link rel="stylesheet" type="text/css" href="fonts/raleway.css">
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
        <div id="intro-download">Download Cypria</div>
      </div>
      <div class="playground section">
        <h1>Try Cypria</h1>
        <form id="code-box">
          <textarea id="code">map (project_cols [name]) (Sailors)</textarea>
        </form>
        <div id="code-output">
          <pre><code class="output">SELECT name FROM (SELECT * FROM (Sailors))</code></pre>
        </div>
        <button id="run-button">Run code</button>
      </div>
      <div class="links section">
        <h1>Documentation & links</h1>
        <?php include("includes/links.php") ?>
      </div>
      <div class="other section">
        <h1>News</h1>
        <?php include("includes/news.php") ?>
      </div>
    </div>

    <!-- Footer -->
    <footer>
      <?php include("includes/footer.php"); ?>
    </footer>
  </div>
</body>

</html>