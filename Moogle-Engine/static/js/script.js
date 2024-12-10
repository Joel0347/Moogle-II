function search() {
    const query = document.getElementById('query').value;
    fetch(`/search/${query}`)
        .then(response => response.json())
        .then(data => {
            const resultsDiv = document.getElementById('results');
            resultsDiv.innerHTML = '';
            data.forEach(result => {
                const p = document.createElement('p');
                p.textContent = result;
                resultsDiv.appendChild(p);
            });
        })
        .catch(error => console.error('Error:', error));
}