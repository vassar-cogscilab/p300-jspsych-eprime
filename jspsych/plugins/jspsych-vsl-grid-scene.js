/**
 * jsPsych plugin for showing scenes that mimic the experiments described in
 *
 * Fiser, J., & Aslin, R. N. (2001). Unsupervised statistical learning of
 * higher-order spatial structures from visual scenes. Psychological science,
 * 12(6), 499-504.
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['vsl-grid-scene'] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('vsl-grid-scene', 'stimuli', 'image');

  plugin.info = {
    name: 'vsl-grid-scene',
    description: '',
    parameters: {
      stimuli: {
        type: [jsPsych.plugins.parameterType.STRING],
        array: true,
        default: undefined,
        no_function: false,
        description: ''
      },
      image_size: {
        type: [jsPsych.plugins.parameterType.INT],
        array: true,
        default: [100,100],
        no_function: false,
        description: ''
      },
      timing_duration: {
        type: [jsPsych.plugins.parameterType.INT],
        default: 2000,
        no_function: false,
        description: ''
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    // default parameter values
    trial.image_size = trial.image_size || [100, 100];
    trial.timing_duration = typeof trial.timing_duration === 'undefined' ? 2000 : trial.timing_duration;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    display_element.innerHTML = plugin.generate_stimulus(trial.stimuli, trial.image_size);

    jsPsych.pluginAPI.setTimeout(function() {
      endTrial();
    }, trial.timing_duration);

    function endTrial() {

      display_element.innerHTML = '';

      var trial_data = {
        "stimulus": JSON.stringify(trial.stimuli)
      };

      jsPsych.finishTrial(trial_data);
    }
  };

  plugin.generate_stimulus = function(pattern, image_size) {
    var nrows = pattern.length;
    var ncols = pattern[0].length;

    // create blank element to hold code that we generate
    var html = '<div id="jspsych-vsl-grid-scene-dummy" css="display: none;">';

    // create table
    html += '<table id="jspsych-vsl-grid-scene table" '+
      'style="border-collapse: collapse; margin-left: auto; margin-right: auto;">';

    for (var row = 0; row < nrows; row++) {
      html += '<tr id="jspsych-vsl-grid-scene-table-row-'+row+'" css="height: '+image_size[1]+'px;">';

      for (var col = 0; col < ncols; col++) {
        html += '<td id="jspsych-vsl-grid-scene-table-' + row + '-' + col +'" '+
          'style="padding: '+ (image_size[1] / 10) + 'px ' + (image_size[0] / 10) + 'px; border: 1px solid #555;">'+
          '<div id="jspsych-vsl-grid-scene-table-cell-' + row + '-' + col + '" style="width: '+image_size[0]+'px; height: '+image_size[1]+'px;">';
        if (pattern[row][col] !== 0) {
          html += '<img '+
            'src="'+pattern[row][col]+'" style="width: '+image_size[0]+'px; height: '+image_size[1]+'"></img>';
        }
        html += '</div>';
        html += '</td>';
      }
      html += '</tr>';
    }

    html += '</table>';
    html += '</div>';

    return html;

  };

  return plugin;
})();
