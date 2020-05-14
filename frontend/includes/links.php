<?php
function make_links($links, $titles)
{ ?>
  <ul>
    <?php for ($i = 0; $i < count($links); $i++) { ?>
      <li>
        <a href="<?php echo $links[$i]; ?>">
          <?php echo $titles[$i]; ?>
        </a>
      </li>
    <?php } ?>
  </ul>
<?php
}
$links = [
  "https://github.com/CornellDataScience/cypria-query-language/blob/master/cypria_planning/language_spec.md",
  "https://github.com/CornellDataScience/cypria-query-language",
  "https://cornelldata.science"
];

$titles = [
  "Cypria documentation",
  "Open-source code",
  "Cornell Data Science"
];

make_links($links, $titles);
?>