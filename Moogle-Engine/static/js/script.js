document.addEventListener('DOMContentLoaded', function() {
    const searchInput = document.getElementById('search-input');
    const fileList = document.getElementById('file-list');

    // Función para cargar y filtrar los archivos desde el servidor
    function loadFiles(query = '') {
        fetch('/files')
            .then(response => response.json())
            .then(files => {
                fileList.innerHTML = ''; // Limpiar la lista antes de agregar nuevos elementos
                const filteredFiles = files.filter(file => file.toLowerCase().includes(query.toLowerCase()));

                // Si no hay archivos, mostrar un mensaje
                if (filteredFiles.length === 0) {
                    const li = document.createElement('li');
                    li.innerHTML = `<span>No se encontraron archivos que coincidan con "${query}"</span>`;
                    fileList.appendChild(li);
                } else {
                    // Agregar los archivos filtrados a la lista
                    filteredFiles.forEach(file => {
                        const li = document.createElement('li');
                        li.innerHTML = `
                            <span>${file}</span>
                            <a href="/download/${file}" class="download-link">Descargar</a>
                        `;
                        fileList.appendChild(li);
                    });
                }
            })
            .catch(error => {
                console.error("Error al cargar los archivos:", error);
            });
    }

    // Cargar los archivos inicialmente
    loadFiles();

    // Filtrar los archivos según la entrada en la barra de búsqueda al presionar Enter
    searchInput.addEventListener('keypress', function(event) {
        if (event.key === 'Enter') {
            loadFiles(searchInput.value); // Llamar a loadFiles con el valor de la barra de búsqueda
        }
    });
});
