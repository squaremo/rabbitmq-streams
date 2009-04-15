package net.lshift.feedshub.harness;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import net.sf.json.JSONObject;

public class Run {

	public static void main(final String[] args) throws IOException {
		JSONObject jsonArgs = JSONObject.fromObject(new BufferedReader(new InputStreamReader(System.in)).readLine());
		System.out.println(jsonArgs.toString());
		
	}
}
