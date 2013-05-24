module.exports = function(grunt) {

  grunt.loadNpmTasks('grunt-rigger');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-mocha-cov');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-hogan');

  // Project configuration.
  grunt.initConfig({
    meta: {
      version: '0.1.0',
      banner: '/*! Dictionary Maker - v<%= meta.version %> - ' + '<%= grunt.template.today("yyyy-mm-dd") %>\n' + '* http://ling.wisc.edu/\n' + '* Copyright (c) <%= grunt.template.today("yyyy") %> ' + 'UW Madison Board of Regents; Licensed GNU GPLv3 */'
    },
    rig: {
      'priv/www/application.js': 'priv/src/client/application.js',
      'priv/test/server/validation.js': 'priv/test/couch_validation.js',
      'priv/test/server/fields.js': 'priv/test/couch_fields.js',
      'priv/test/server/get_changes.js': 'priv/test/couch_get_changes.js',
      'priv/test/server/user_indexes.js': 'priv/test/couch_user_indexes.js'
    },
    mochacov: {
      unit: {
        options: {
          reporter: 'spec'
        }
      },
      coverage: {
        options: {
          reporter: 'html-cov',
          output: 'coverage.html',
          coverage: true
        }
      },
      options: {
        ui: 'bdd',
        files: 'priv/test/server/*.js'
      }
    },
    uglify: {
      all: {
        files: {
          'priv/www/application.min.js': 'priv/www/application.js',
          'priv/www/templates.min.js': 'priv/www/templates.js'
        }
      }
    },
    less: {
      all: {
        files: {
          "priv/www/css/screen.css": "priv/less/screen.less"
        }
      }
    },
    hogan: {
      all: {
        templates: "priv/templates/*.mustache",
        output: "priv/www/templates.js",
        binderName: "hulk"
      }
    },
    jshint: {
      options: {
        boss: true,
        browser: true,
        curly: true,
        eqeqeq: true,
        immed: true,
        indent: 2,
        latedef: true,
        maxdepth: 3,
        newcap: true,
        noarg: true,
        nonew: true,
        //plusplus: true,
        quotmark: 'single',
        sub: true,
        strict: true,
        trailing: true,
        undef: true,
        globals: {
          $: true,
          describe: true,
          emit: true,
          exports: true,
          getRow: true,
          Hogan: true,
          it: true,
          jQuery: true,
          require: true,
          send: true,
          shimi: true,
          start: true,
          templates: true
        }
      },
      all: ['grunt.js', 'priv/src/**/*.js', 'priv/test/fixtures/*.js']
    }
  });

  // Default task.
  grunt.registerTask('default', ['less', 'hogan', 'jshint', 'rig', 'mochacov:unit', 'mochacov:coverage', 'uglify']);
};
