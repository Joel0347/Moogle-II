document.getElementById('search-form').addEventListener('submit', function(event) {
    event.preventDefault();
    const query = document.querySelector('input[name="q"]').value;
    const page = document.querySelector('input[name="page"]').value;
    console.log('Enviando consulta:', query);
    fetchResults(query, page);
});

function fetchResults(query, page) {
    fetch(`/search?q=${query}&page=${page}`)
        .then(response => {
            if (!response.ok) {
                throw new Error('Network response was not ok');
            }
            return response.text();
        })
        .then(data => {
            console.log('Respuesta recibida:', data);
            const resultsDiv = document.getElementById('results');
            resultsDiv.innerHTML = data; // Mostrar el texto de respuesta
            updatePagination(query, page);
        })
        .catch(error => {
            console.error('Hubo un problema con la operación fetch:', error);
        });
}

function updatePagination(query, page) {
    const pagination = document.getElementById('pagination');
    pagination.innerHTML = '';

    const prevPage = Math.max(1, parseInt(page) - 1);
    const nextPage = parseInt(page) + 1;

    const prevLink = document.createElement('a');
    prevLink.href = `javascript:fetchResults('${query}', '${prevPage}')`;
    prevLink.innerText = 'Anterior';
    const prevItem = document.createElement('li');
    prevItem.appendChild(prevLink);

    const nextLink = document.createElement('a');
    nextLink.href = `javascript:fetchResults('${query}', '${nextPage}')`;
    nextLink.innerText = 'Siguiente';
    const nextItem = document.createElement('li');
    nextItem.appendChild(nextLink);

    pagination.appendChild(prevItem);
    pagination.appendChild(nextItem);
}
