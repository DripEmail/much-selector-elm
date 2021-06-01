describe("My First Test", () => {
  it("Visits the Kitchen Sink", () => {
    cy.visit("http://localhost:1234");

    cy.contains("Option Groups");
  });
});
