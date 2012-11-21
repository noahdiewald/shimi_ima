/*global module:false*/
module.exports = function(grunt) {

  grunt.loadNpmTasks('grunt-rigger');

  // Project configuration.
  grunt.initConfig({
    meta: {
      version: '0.1.0',
      banner: '/*! Dictionary Maker - v<%= meta.version %> - ' +
        '<%= grunt.template.today("yyyy-mm-dd") %>\n' +
        '* http://ling.wisc.edu/\n' +
        '* Copyright (c) <%= grunt.template.today("yyyy") %> ' +
        'UW Madison Board of Regents; Licensed GNU GPLv3 */'
    },
    rig: {
      dist: {
        src: 'priv/www/js/application.js',
        dest: 'priv/www/application.js'
      }
    },
    lint: {
      files: ['grunt.js', 'priv/www/js/**/*.js']
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
        path: true,
        store: true,
        searches: true,
        eui: true,
        vui: true,
        iui: true,
        index: true
      }
    },
    uglify: {}
  });

  // Default task.
  grunt.registerTask('default', 'lint rig min');

};
