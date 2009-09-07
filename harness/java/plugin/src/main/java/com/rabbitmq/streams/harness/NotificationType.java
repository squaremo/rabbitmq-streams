package com.rabbitmq.streams.harness;

public enum NotificationType {
  // *** informational NotificationTypes ***
  Startup, Shutdown, StopNotifier,
  // *** error related NotificationTypes ***
  /** A plugin (most likely a pipeline component) expected data but received none. */
  NoData,
  /** A plugin (most likely a pipeline component) received illegal data.*/
  BadData,
  /** Something that is in the domain of rabbit-streams itself went badly wrong
   * and requires human intervention. 
   * 
   * Examples:
   * - A misconfigured plugin.
   * - A plugin can't access rabbit-streams resources like its database or state.
   */
  FatalError,
  /** Something outside the domain of rabbit-streams itself went wrong and as a
   * result some required resource or serive is not availabe. This will also usually
   * require human intervention. This should normally only occur in servers.
   *
   * Examples:
   * 
   * - A XMPP gateway plugin can't connect to the XMPP server.
   * - Some external document or URL is missing or can't be retrieved.
   */
  Unavailable
};
