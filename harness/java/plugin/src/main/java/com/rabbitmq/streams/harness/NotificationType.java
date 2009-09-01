package com.rabbitmq.streams.harness;
/**
 * Unavailable refers to external resources not working/existing e.g. servers or files.
 */
public enum NotificationType { Startup, Shutdown, StopNotifier, NoData, BadData, FatalError, Unavailable };