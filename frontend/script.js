document.getElementById('uploadForm').addEventListener('submit', function(event) {
    event.preventDefault(); // Prevent the default form submit

    var fileInput = document.getElementById('fileInput');
    var file = fileInput.files[0];
    var formData = new FormData();
    formData.append('file', file);

    // Here you would send formData to the backend
    // For now, let's just log it to the console
    console.log(formData);

    // TODO: Add fetch API call to send the file to the backend
});
