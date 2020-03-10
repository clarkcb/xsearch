'use strict';

const gulp = require('gulp');

var del = require('del');
var rename = require('gulp-rename');
var replace = require('gulp-replace');
var uglify = require('gulp-uglify');

var DEST = 'build/';

function clean(cb) {
  del(['./build']);
  cb();
}

function build(cb) {
  gulp.src('./src/*.js', { sourcemaps: true })
      .pipe(replace(/require\('\.\/(\w+)'\)/g, "require('./$1.min.js')"))
      //.pipe(uglify())
      .pipe(rename({ extname: '.min.js' }))
      .pipe(gulp.dest(DEST, { sourcemaps: true }));
  cb();
}

exports.clean = clean;
exports.build = build;
exports.default = gulp.series(clean, build);
