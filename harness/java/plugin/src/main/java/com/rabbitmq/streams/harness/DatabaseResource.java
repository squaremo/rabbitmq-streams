/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import java.io.IOException;
import net.sf.json.JSONObject;

/**
 * An abstraction of a document database.
 * @author mikeb@lshift.net
 */
public interface DatabaseResource {

  public JSONObject getDocument(String id) throws IOException;
  public void saveDocument(JSONObject doc, String id) throws IOException;
  public String getName();

}
