'use strict';

var gulp = require('gulp');

var del = require('del');
var rename = require('gulp-rename');
var replace = require('gulp-replace');
var uglify = require('gulp-uglify');

var DEST = 'build/';

gulp.task('clean', function () {  
  return del(['./build']);
});

gulp.task('build', function() {
  del(['./build'], function(err) {
    if (err) return;
    return gulp.src('./src/*.js')
      .pipe(replace(/require\('\.\/(\w+)\.js'\)/g, "require('./$1.min.js')"))
      .pipe(uglify())
      .pipe(rename({ extname: '.min.js' }))
      .pipe(gulp.dest(DEST));
  });
});

gulp.task('default', ['build'], function() {
});
