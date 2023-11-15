const { ContractTemplate, ContractTemplateObj } = require("sigmastate-js/main");

describe("Smoke tests for API exporting", () => {
  it("Should export ContractTempate object", () => {
    expect(ContractTemplate).not.toBeUndefined();
  });
});

describe("ContractTemplate", () => {
  let templateJsonStr = "{\n" +
      "  \"treeVersion\" : null,\n" +
      "  \"name\" : \"TestContractTemplate\",\n" +
      "  \"description\" : \"TestContractTemplateDescription\",\n" +
      "  \"constTypes\" : [\n" +
      "    \"02\",\n" +
      "    \"02\",\n" +
      "    \"02\"\n" +
      "  ],\n" +
      "  \"constValues\" : [\n" +
      "    10,\n" +
      "    20,\n" +
      "    30\n" +
      "  ],\n" +
      "  \"parameters\" : [\n" +
      "    {\n" +
      "      \"name\" : \"p1\",\n" +
      "      \"description\" : \"p1_description\",\n" +
      "      \"constantIndex\" : 0\n" +
      "    },\n" +
      "    {\n" +
      "      \"name\" : \"p2\",\n" +
      "      \"description\" : \"p2_description\",\n" +
      "      \"constantIndex\" : 1\n" +
      "    },\n" +
      "    {\n" +
      "      \"name\" : \"p3\",\n" +
      "      \"description\" : \"p3_description\",\n" +
      "      \"constantIndex\" : 2\n" +
      "    }\n" +
      "  ],\n" +
      "  \"expressionTree\" : \"d1939a730073017302\"\n" +
      "}";

  it("roundtrip for P2PK", () => {
    let template = ContractTemplateObj.fromJsonString(templateJsonStr);
    expect(template).not.toBeUndefined();
    expect(template.toJsonString()).toEqual(templateJsonStr);
    // expect(addr.isP2PK()).toEqual(true)
    //
    // expect(addr.isP2S()).toEqual(false)
    // expect(addr.toString()).toEqual(addrStr)
    // expect(addr.asP2PK()).not.toBeUndefined();
  });

});

