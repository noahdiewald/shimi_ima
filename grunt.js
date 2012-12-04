/*global module:false*/
module.exports = function (grunt) {

  grunt.loadNpmTasks('grunt-rigger');
  grunt.loadNpmTasks('grunt-beautify');
  grunt.loadNpmTasks('grunt-simple-mocha');
  grunt.loadNpmTasks('grunt-contrib-less');

  // Project configuration.
  grunt.initConfig({
    meta: {
      version: '0.1.0',
      banner: '/*! Dictionary Maker - v<%= meta.version %> - ' + '<%= grunt.template.today("yyyy-mm-dd") %>\n' + '* http://ling.wisc.edu/\n' + '* Copyright (c) <%= grunt.template.today("yyyy") %> ' + 'UW Madison Board of Regents; Licensed GNU GPLv3 */'
    },
    rig: {
      'priv/www/application.js': 'priv/src/client/application.js',
      'priv/test/server/validation.js': 'priv/test/couch_validation.js'
    },
    lint: {
      files: ['grunt.js', 'priv/src/**/*.js', 'priv/test/fixtures/*.js']
    },
    simplemocha: {
      all: {
        src: 'priv/test/server/*.js',
        options: {
          globals: ['should']
        }
      }
    },
    beautify: {
      files: ['priv/src/**/*.js', 'priv/test/fixtures/*.js']
    },
    beautifier: {
      options: {
        indentSize: 2
      }
    },
    min: {
      dist: {
        src: ['<banner:meta.banner>', 'priv/www/application.js'],
        dest: 'priv/www/application.min.js'
      }
    },
    less: {
      all: {
        files: {
          "priv/www/css/screen.css": "priv/less/screen.less"
        }
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
        start: true,
        describe: true,
        it: true,
        require: true,
        exports: true
      }
    },
    uglify: {}
  });

  // Default task.
  grunt.registerTask('default', 'beautify lint rig simplemocha min less');

};
