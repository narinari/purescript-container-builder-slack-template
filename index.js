const NotifyContainerBuild = require('./output/NotifyContainerBuild');

exports.notifyBuildPubSub = function(message, callback) {
  return NotifyContainerBuild.notifyBuildPubSub(message)(callback)();
}