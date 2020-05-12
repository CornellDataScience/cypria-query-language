/** CDS 2020 */

/**
 * Backend controller for Cypria language server.
 */
/** Imports */
const express = require('express');
const bodyParser = require('body-parser');
const hbs = require('hbs');
var router = express.Router();
var engine = require('consolidate');
var child_p = require('child_process');
var cors = require('cors');
const fs = require('fs');
const hash = require('hash.js');

const port = process.env.PORT || 80;
const app = express();
app.use(bodyParser());
app.set('views', __dirname);  // making ./views as the views directory
app.use(express.json());
app.use('/css', express.static('./css'))
app.use('/js', express.static('./js'))
app.use('/vendor', express.static('./vendor'))
app.use('/scss', express.static('./scss'))
app.use('/img', express.static('./img'))
app.use('/bs-platform/lib/js', express.static('./bs-platform/lib/js'))
app.use('/highlight', express.static('./highlight'))
app.use(express.urlencoded({ extended: false }));
app.use(cors());
var path = __dirname;
const MAX_LESSON = 4;

router.use(function (req, res, next) {
  console.log('/' + req.method);
  next();
});

router.get('/client.js', function (req, res) {
  res.sendFile(path + '/client.js')
});

router.get('/', function (req, res) {
  res.sendFile(path + '/index.html');
});

function postCompileJs(js_code) {
  return post_compile.post_compile(js_code);
}

async function compileFromCypriaToSQL(cypria, res) {
  console.log("<compileFromCypriaToSQL>: call");
  child_p
    .exec(
      '"../main.byte" ',
      (error, stdout, stderr) => {
        console.log("<compileFromCypriaToSQL>: child process terminated");
        if (error) {
          res.send('Ran into an error compiling your code:\n' + stderr);
          return;
        }
        console.log(stdout);
        res.send(stdout);
      })
    .stdin.write(cypria + "\n");
  console.log("<compileFromCypriaToSQL>: stdin written");
}

app.post('/cypria', async (req, res) => {
  console.log("<app.post>: POST request to /cypria endpoint.");
  var cypria_raw = req.body.cypria_raw;
  try {
    await compileFromCypriaToSQL(cypria_raw, res)
  } catch (e) {
    console.log(e.message);
    res.send('Hit an unexpected error when compiling Cypria.');
  }
});

async function getOcamlLesson(lesson_number, res) {
  filename = 'lesson' + lesson_number + '.ml'
  fs.readFile('lessons/' + filename, (err, data) => {
    if (err) throw err
    res.send(data)
  });
}

app.post('/ocaml_lesson', async (req, res) => {
  var lesson_number = req.body.lesson_number;
  try {
    await getOcamlLesson(lesson_number, res)
  } catch (e) {
    res.send('Hit an unexpected error when grabbing the lesson.');
    throw e;
  }
});

// GET / route for serving index.html file
app.use('/', router);

// To make the server live
app.listen(port, () => {
  console.log(`CDS Cypria Lang Server is live on port ${port}`);
});
