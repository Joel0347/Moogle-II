document.addEventListener('DOMContentLoaded', function() {
    const searchInput = document.getElementById('search-input');
    const fileList = document.getElementById('file-list');

    function loadFiles(query = '') {
        if (!query) {
            // Si no hay consulta, mostrar todos los archivos
            fetch('/files')
                .then(response => response.json())
                .then(files => displayFiles(files))
                .catch(error => console.error("Error:", error));
        } else {
            // Si hay consulta, usar la búsqueda TF-IDF
            fetch(`/search?q=${encodeURIComponent(query)}`)
                .then(response => response.json())
                .then(results => {
                    fileList.innerHTML = '';
                    if (results.length === 0) {
                        const li = document.createElement('li');
                        li.innerHTML = `<span>No se encontraron resultados para "${query}"</span>`;
                        fileList.appendChild(li);
                    } else {
                        results.forEach(result => {
                            const li = document.createElement('li');
                            li.innerHTML = `
                                <span>${result.filename} (Relevancia: ${result.score.toFixed(4)})</span>
                                <a href="/download/${result.filename}" class="download-link">Descargar</a>
                            `;
                            fileList.appendChild(li);
                        });
                    }
                })
                .catch(error => console.error("Error:", error));
        }
    }

    function displayFiles(files) {
        fileList.innerHTML = '';
        if (files.length === 0) {
            const li = document.createElement('li');
            li.innerHTML = '<span>No hay archivos disponibles</span>';
            fileList.appendChild(li);
        } else {
            files.forEach(file => {
                const li = document.createElement('li');
                li.innerHTML = `
                    <span>${file}</span>
                    <a href="/download/${file}" class="download-link">Descargar</a>
                `;
                fileList.appendChild(li);
            });
        }
    }

    // Cargar archivos inicialmente
    loadFiles();

    // Manejar búsqueda
    searchInput.addEventListener('keypress', function(event) {
        if (event.key === 'Enter') {
            loadFiles(searchInput.value);
        }
    });
});
