package com.rabbitmq.streams.harness;

public class PluginException extends Exception  {

  public PluginException(String message)  {
    super(message);
  }

  public PluginException(Throwable throwable) {
    super(throwable);
  }

  public PluginException(String message, Throwable throwable) {
    super(message, throwable);
  }
}
