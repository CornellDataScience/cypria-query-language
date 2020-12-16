<?php
function generate_rows($num)
{
  for ($i = 1; $i <= $num; $i++) { ?>
    <div><?php echo $i; ?></div>
<?php }
}
?>

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
  <link rel="stylesheet" type="text/css" href="css/playground.css">
  <link rel="stylesheet" type="text/css" href="css/banner.css">
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
    <?php include("includes/playground-banner.php"); ?>
  </nav>

  <!-- Content -->
  <div class="main-container">
    <form id="code-box">
      <textarea class="code-text" id="code">filter ($count>3$) (Sailors)</textarea>
    </form>
    <div id="code-output">
      <pre><code class="output">SELECT * FROM (SELECT * FROM (Sailors)) WHERE (count>3)</code></pre>
    </div>
  </div>
</body>

</html>