package com.rabbitmq.streams.plugins.regexp.split;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.NotificationType;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.PluginException;
import java.nio.charset.CharacterCodingException;
import net.sf.json.JSONObject;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexpSplit extends PipelineComponent {

  final static String POSITIVE = "positive";
  final static String NEGATIVE = "negative";
  private Pattern pattern;

  public void configure(final JSONObject config) throws PluginBuildException {
    if (null == config) {
      throw new PluginBuildException("Cannot configure a plugin with a null configuration.");
    }

    String regexp = config.getString("regexp");
    int flags = (config.optBoolean("multiline", false) ? Pattern.MULTILINE : 0)
      | (config.optBoolean("caseinsensitive", false) ? Pattern.CASE_INSENSITIVE : 0)
      | (config.optBoolean("dotall", false) ? Pattern.DOTALL : 0)
      | Pattern.UNICODE_CASE; // FIXME: should we add Pattern.CANON_EQ?
    pattern = Pattern.compile(regexp, flags);
    registerInput("input", input);
  }

  InputReader input = new InputReader() {

    @Override
    public void handleMessage(InputMessage msg) throws PluginException {
        Matcher matcher;
      try {
        matcher = pattern.matcher(msg.bodyAsString());
      } catch (CharacterCodingException ex) {
        notifier.notify(NotificationType.BadData, "msg.body isn't valid utf-8");
        throw new PluginException("Msg.body isn't valid utf-8", ex);
      }
        if (matcher.matches()) {
          publishToChannel(POSITIVE, msg);
        }
        else {
          publishToChannel(NEGATIVE, msg);
        }
    }
  };
}
