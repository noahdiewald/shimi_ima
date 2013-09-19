module.exports = function(grunt) {

  grunt.loadNpmTasks('grunt-browserify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-docco');
  grunt.loadNpmTasks('grunt-hogan');
  grunt.loadNpmTasks('grunt-mocha-cov');

  // Project configuration.
  grunt.initConfig({
    meta: {
        version: '0.1.0',
        banner: '/*! Dictionary Maker - v<%= meta.version %> - ' + '<%= grunt.template.today("yyyy-mm-dd") %>\n' + '* http://ling.wisc.edu/\n' + '* Copyright (c) <%= grunt.template.today("yyyy") %> ' + 'UW Madison Board of Regents; Licensed GNU GPLv3 */'
    },
    browserify: {
      client: {
        src: ['priv/src/client/**/*.js'],
        dest: 'priv/www/application.tmp.js'
      }
    },
    concat: {
      dist: {
        src: ['priv/src/client/globals.js', '<%= browserify.client.dest %>'],
        dest: 'priv/www/application.js'
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
          files: ['priv/test/server/*.js', 'priv/test/misc/*.js'],
          reporter: 'spec'
        }
      },
      coverage: {
        options: {
          reporter: 'html-cov',
          output: 'coverage.html',
          files: 'priv/test/server/*.js',
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
          'priv/www/application.min.js': '<%= concat.dist.dest %>',
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

  grunt.registerTask('coverage', ['jshint', 'mochacov:coverage']);
  grunt.registerTask('test', ['jshint', 'mochacov:unit']);
  grunt.registerTask('default', ['less', 'hogan', 'jshint', 'mochacov:unit', 'browserify', 'concat', 'uglify']);
};
