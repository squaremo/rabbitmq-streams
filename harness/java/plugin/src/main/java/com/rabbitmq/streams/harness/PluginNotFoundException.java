package com.rabbitmq.streams.harness;

/**
 *
 * @author mikeb
 */
class PluginNotFoundException extends PluginBuildException {

  public PluginNotFoundException(String msg) {
    super(msg);
  }

  public PluginNotFoundException(String msg, Exception ex) {
    super(msg, ex);
  }

}
