module com.example.GUIA3 {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.GUIA3 to javafx.fxml;
    exports com.example.GUIA3;
}