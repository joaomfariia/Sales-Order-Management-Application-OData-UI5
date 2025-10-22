sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/m/MessageToast"
], (Controller) => {
    "use strict";

    return Controller.extend("zsomanage.controller.Sales", {
        onInit() {
        },

        onApprove() {
            var sApprovedMsg = this.getView().getModel("i18n").getResourceBundle().getText("approvedSalesOrder");
            sap.m.MessageToast.show(sApprovedMsg);
        },

        onReject() {
            var sRejectedMsg = this.getView().getModel("i18n").getResourceBundle().getText("rejectedSalesOrder");
            sap.m.MessageToast.show(sRejectedMsg);
        }
    });
});