package com.rabbitmq.streams.plugins.regexp.replace;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.NotificationType;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.PluginException;
import java.nio.charset.CharacterCodingException;
import net.sf.json.*;

import java.util.LinkedList;
import java.util.ListIterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class RegexpReplace extends PipelineComponent {

  private class PatRep {
     public Pattern pat; public String rep;
     public PatRep(Pattern pat, String rep) {this.rep=rep; this.pat=pat;}
  }
  private LinkedList<PatRep> patReps = new LinkedList();

  public void configure(final JSONObject config) throws PluginBuildException {
    if (null == config) {
      throw new PluginBuildException("Cannot configure a plugin with a null configuration.");
    }
    JSONArray rexConfigs = config.getJSONArray("expressions");
    for (ListIterator iterator = rexConfigs.listIterator(rexConfigs.size()); iterator.hasPrevious();) {
      JSONObject rexConfig = (JSONObject)iterator.previous();
      String regexp = rexConfig.getString("regexp");
      int flags = (rexConfig.optBoolean("multiline", false) ? Pattern.MULTILINE : 0)
                 | (rexConfig.optBoolean("caseinsensitive", false) ? Pattern.CASE_INSENSITIVE : 0)
                 | (rexConfig.optBoolean("dotall", false) ? Pattern.DOTALL : 0)
                 | Pattern.UNICODE_CASE;
      patReps.addFirst(new PatRep(Pattern.compile(regexp, flags), rexConfig.getString("replacement")));
    }
    registerInput("input", input);
  }

  InputReader input = new InputReader() {

    @Override
    public void handleMessage(InputMessage msg) throws PluginException {
      Matcher matcher;
      String bodySoFar = null;
      try{
        bodySoFar = msg.bodyAsString();
      } catch (CharacterCodingException ex) {
        notifier.notify(NotificationType.BadData, "msg.body isn't valid utf-8");
        throw new PluginException("Msg.body isn't valid utf-8", ex);
      }
      for (PatRep patRep : patReps) {
        bodySoFar = patRep.pat.matcher(bodySoFar).replaceAll(patRep.rep);
      }
      publishToChannel("output", msg.withBody(bodySoFar));
    }
  };
}
