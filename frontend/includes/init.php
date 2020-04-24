<?php
// Check to see if PHP >= version 7
function check_php_version()
{
  if (version_compare(phpversion(), '7.0', '<')) {
    define('VERSION_MESSAGE', "PHP version 7.0 or higher is recommended.");
    echo VERSION_MESSAGE;
    throw VERSION_MESSAGE;
  }
}
// check_php_version();

function config_php_errors()
{
  ini_set('display_startup_errors', 1);
  ini_set('display_errors', 0);
  error_reporting(E_ALL);
}
config_php_errors();

// Open a connection to an SQLite database stored in filename: $db_filename.
// Returns: Connection to database.
function open_sqlite_db($db_filename)
{
  $db = new PDO('sqlite:' . $db_filename);
  $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
  return $db;
}

// Execute a query ($sql) against a datbase ($db).
// Returns query results if query was successful.
// Returns null if query was not successful.
function exec_sql_query($db, $sql, $params = array())
{
  $query = $db->prepare($sql);
  if ($query and $query->execute($params)) {
    return $query;
  }
  return null;
}
