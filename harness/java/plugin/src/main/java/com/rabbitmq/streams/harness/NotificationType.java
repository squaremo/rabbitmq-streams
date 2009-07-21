package com.rabbitmq.streams.harness;

public enum NotificationType {

  Startup("Startup"), Shutdown("Shutdown"), NoData("NoData"), BadData("BadData"), FatalError("FatalError"), StopNotifier("StopNotifier");

  private String label;

  NotificationType(String label)  {
    this.label = label;
  }

  public String getLabel() {
    return label;
  }
}
