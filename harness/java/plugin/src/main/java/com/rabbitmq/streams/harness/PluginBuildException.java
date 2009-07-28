/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

/**
 *
 * @author mikeb@lshift.net
 */
public class PluginBuildException extends Exception {

  public PluginBuildException(String msg) {
    super(msg);
  }

  public PluginBuildException(String msg, Exception ex) {
    super(msg, ex);
  }

}
