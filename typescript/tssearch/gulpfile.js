'use strict';

var gulp = require('gulp');

var del = require('del');
var rename = require('gulp-rename');
var replace = require('gulp-replace');
var ts = require('gulp-typescript');
var uglify = require('gulp-uglify');

var DEST = './build/';

gulp.task('clean', function () {  
  return del([DEST]);
});

gulp.task('build', function () {
  del([DEST], function(err) {
    if (err) return;
    var tsResult = gulp.src('./src/*.ts')
      .pipe(ts({
          noImplicitAny: true
          // out: 'output.js'
        }));
    return tsResult.js
      .pipe(gulp.dest(DEST));
  });
});

gulp.task('build-min', function () {
  del([DEST], function(err) {
    if (err) return;
    var tsResult = gulp.src('./src/*.ts')
      .pipe(replace(/require\('\.\/(\w+)\.js'\)/g, "require('./$1.min.js')"))
      .pipe(ts({
          noImplicitAny: true
          // out: 'output.js'
        }));
    return tsResult.js
      .pipe(uglify())
      .pipe(rename({ extname: '.min.js' }))
      .pipe(gulp.dest(DEST));
  });
});

gulp.task('default', ['build'], function() {
});
