'use strict';

var gulp = require('gulp');

var del = require('del');
var rename = require('gulp-rename');
var replace = require('gulp-replace');
var ts = require('gulp-typescript');
var uglify = require('gulp-uglify');

var BUILD = './build/';
var SRC   = './src/';
var TESTS = './tests/';

gulp.task('clean', function () {  
  return del([BUILD]);
});

gulp.task('build', function () {
  del([BUILD], function(err) {
    if (err) return;
    var tsResult = gulp.src(SRC + '*.ts')
      .pipe(replace(/require\('\.\/(\w+)\.ts'\)/g, "require('./$1.js')"))
      .pipe(ts({
          //noImplicitAny: true
          // out: 'output.js'
        }));
    return tsResult.js
      .pipe(gulp.dest(BUILD));
  });
});

gulp.task('build-min', function () {
  del([BUILD], function(err) {
    if (err) return;
    var tsResult = gulp.src(SRC + '*.ts')
      .pipe(replace(/require\('\.\/(\w+)\.ts'\)/g, "require('./$1.min.js')"))
      .pipe(ts({
          noImplicitAny: true
          // out: 'output.js'
        }));
    return tsResult.js
      .pipe(uglify())
      .pipe(rename({ extname: '.min.js' }))
      .pipe(gulp.dest(BUILD));
  });
});

gulp.task('clean-tests', function () {  
  return del([TESTS + '*.js']);
});

gulp.task('build-tests', function () {
  del([TESTS + '*.js'], function(err) {
    if (err) return;
    var tsResult = gulp.src(TESTS + '*.ts')
      .pipe(ts({
          noImplicitAny: true
          // out: 'output.js'
        }));
    return tsResult.js
      .pipe(gulp.dest(TESTS));
  });
});

gulp.task('default', ['build'], function() {
});
