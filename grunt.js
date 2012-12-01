/*global module:false*/
module.exports = function (grunt) {

  grunt.loadNpmTasks('grunt-rigger');
  grunt.loadNpmTasks('grunt-beautify');

  // Project configuration.
  grunt.initConfig({
    meta: {
      version: '0.1.0',
      banner: '/*! Dictionary Maker - v<%= meta.version %> - ' + '<%= grunt.template.today("yyyy-mm-dd") %>\n' + '* http://ling.wisc.edu/\n' + '* Copyright (c) <%= grunt.template.today("yyyy") %> ' + 'UW Madison Board of Regents; Licensed GNU GPLv3 */'
    },
    rig: {
      dist: {
        src: 'jssrc/client/application.js',
        dest: 'priv/www/application.js'
      }
    },
    lint: {
      files: ['grunt.js', 'jssrc/**/*.js']
    },
    beautify: {
      files: '<config:lint.files>'
    },
    beautifier: {
      options: {
        indentSize: 2
      }
    },
    min: {
      dist: {
        src: ['<banner:meta.banner>', '<config:rig.dist.dest>'],
        dest: 'priv/www/application.min.js'
      }
    },
    watch: {
      files: '<config:lint.files>',
      tasks: 'lint'
    },
    jshint: {
      options: {
        curly: true,
        eqeqeq: true,
        immed: true,
        latedef: true,
        newcap: true,
        noarg: true,
        sub: true,
        undef: true,
        boss: true,
        eqnull: true,
        browser: true
      },
      globals: {
        jQuery: true,
        $: true,
        shimi: true,
        emit: true,
        send: true,
        getRow: true,
        start: true
      }
    },
    uglify: {}
  });

  // Default task.
  grunt.registerTask('default', 'beautify lint rig min');

};