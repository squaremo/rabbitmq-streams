package net.lshift.feedshub.harness;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.SynchronousQueue;

import com.rabbitmq.client.AlreadyClosedException;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.impl.ChannelN;

public class Logger implements Runnable {

    private static final String logExchange = "feedshub/log";

    private final LogMessage shutdownMessage = new LogMessage(
            LogLevel.Shutdown, null);

    private final ChannelN logChannel;
    private final String logRoutingKey;
    private final BasicProperties persistent;

    private final BlockingQueue<LogMessage> logQueue = new SynchronousQueue<LogMessage>();

    public Logger(ChannelN logChannel, String logRoutingKey,
            BasicProperties persistent) {
        this.logChannel = logChannel;
        this.logRoutingKey = logRoutingKey;
        this.persistent = persistent;
    }

    private String throwableToString(Throwable t) {
        StringWriter out = new StringWriter();
        t.printStackTrace(new PrintWriter(out));
        return out.toString();
    }

    public void debug(Throwable t) {
        info(throwableToString(t));
    }

    public void info(Throwable t) {
        info(throwableToString(t));
    }

    public void warn(Throwable t) {
        warn(throwableToString(t));
    }

    public void error(Throwable t) {
        error(throwableToString(t));
    }

    public void fatal(Throwable t) {
        fatal(throwableToString(t));
    }

    public void debug(String message) {
        try {
            logQueue.put(new LogMessage(LogLevel.Debug, message));
        } catch (InterruptedException e) {
            debug(message);
        }
    }

    public void info(String message) {
        try {
            logQueue.put(new LogMessage(LogLevel.Info, message));
        } catch (InterruptedException e) {
            info(message);
        }
    }

    public void warn(String message) {
        try {
            logQueue.put(new LogMessage(LogLevel.Warn, message));
        } catch (InterruptedException e) {
            warn(message);
        }
    }

    public void error(String message) {
        try {
            logQueue.put(new LogMessage(LogLevel.Error, message));
        } catch (InterruptedException e) {
            error(message);
        }
    }

    public void fatal(String message) {
        try {
            logQueue.put(new LogMessage(LogLevel.Fatal, message));
        } catch (InterruptedException e) {
            fatal(message);
        }
    }

    public void shutdown() {
        try {
            logQueue.put(shutdownMessage);
        } catch (InterruptedException e) {
            shutdown();
        }
    }

    public void run() {
        LogMessage msg;
        while (true) {
            msg = null;
            while (null == msg) {
                try {
                    msg = logQueue.take();
                } catch (InterruptedException e) {
                }
            }
            if (msg == shutdownMessage) {
                break;
            } else {
                msg.log();
            }
        }
        if (logChannel.isOpen())
            try {
                logChannel.close();
            } catch (ShutdownSignalException sse) {
            } catch (IOException e) {
            }
    }

    private class LogMessage {
        private final LogLevel level;
        private final String message;

        LogMessage(LogLevel level, String message) {
            this.level = level;
            this.message = message;
        }

        void log() {
            level.log(logChannel, logRoutingKey, persistent, message);
        }
    }

    private static enum LogLevel {
        Info("info"), Warn("warn"), Error("error"), Fatal("fatal"), Shutdown(
                "shutdown"), Debug("debug");

        private final String level;

        LogLevel(String level) {
            this.level = level;
        }

        void log(ChannelN logChannel, String rkTail,
                BasicProperties persistent, String message) {
            try {
                try {
                    logChannel.basicPublish(logExchange, level + rkTail, false,
                            false, persistent, message.getBytes());
                } catch (AlreadyClosedException ace) {
                    System.err.println(level + rkTail + ": " + message);
                    ace.printStackTrace();
                }
            } catch (IOException e) {
                System.err.println(level + rkTail + ": " + message);
                e.printStackTrace();
            }
        }
    }

}
