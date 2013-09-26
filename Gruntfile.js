module.exports = function(grunt) {

  grunt.loadNpmTasks('grunt-browserify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-docco');
  grunt.loadNpmTasks('grunt-hogan');
  grunt.loadNpmTasks('grunt-mocha-cov');

  // Project configuration.
  grunt.initConfig(
  {
    meta: {
      version: '0.1.0',
      banner: '/*! Dictionary Maker - v<%= meta.version %> - ' + '<%= grunt.template.today("yyyy-mm-dd") %>\n' + '* http://ling.wisc.edu/\n' + '* Copyright (c) <%= grunt.template.today("yyyy") %> ' + 'UW Madison Board of Regents; Licensed GNU GPLv3 */\n\n'
    },
    browserify: {
      client: {
        src: ['priv/src/client/**/*.js'],
        dest: 'priv/www/application.tmp.js',
        options: {
          alias: ['priv/templates/compiled/templates.js:templates.js']
        }
      }
    },
    concat: {
      dist: {
        src: ['priv/src/client/globals.js', '<%= browserify.client.dest %>'],
        dest: 'priv/www/application.js'
      }
    },
    copy: {
      test: {
        files: [
          {expand: true,
           flatten: true,
           src: ['priv/templates/compiled/*'],
           dest: 'node_modules'}
        ]
      }
    },
    docco: {
      client: {
        src: ['priv/src/client/**/*.js'],
        options: {
          output: 'jsdocs/'
        }
      }
    },
    mochacov: {
      unit: {
        options: {
          files: ['priv/test/server/*.js',
                  'priv/test/misc/*.js',
                  'priv/test/client/*.js'],
          reporter: 'spec'
        }
      },
      coverage: {
        options: {
          reporter: 'html-cov',
          output: 'coverage.html',
          files: ['priv/test/server/*.js',
                  'priv/test/client/*.js'],
          coverage: true
        }
      },
      options: {
        ui: 'bdd'
      }
    },
    uglify: {
      options: {
        banner: '<%= meta.banner %>'
      },
      all: {
        files: {
          'priv/www/application.min.js': '<%= concat.dist.dest %>'
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
        output: "priv/templates/compiled/templates.js",
        binderName: "nodejs",
        exposeTemplates: true
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
          globals: true,
          Hogan: true,
          it: true,
          jQuery: true,
          module: true,
          require: true,
          send: true,
          start: true,
          templates: true
        }
      },
      all: ['grunt.js', 'priv/src/**/*.js', 'priv/test/**/*.js']
    }
  });

  grunt.registerTask('coverage', ['jshint', 'copy', 'mochacov:coverage']);
  grunt.registerTask('test', ['jshint', 'copy', 'mochacov:unit']);
  grunt.registerTask('default', ['less', 'hogan', 'jshint', 'copy', 'mochacov:unit', 'browserify', 'concat', 'uglify']);
};
